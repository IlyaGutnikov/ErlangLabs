-module(rss_parse).

-export([is_rss2_feed/1, compare_feed_items/2]).
-export([get_feed_items_test/1, get_item_time_test/1, compare_RSS2_test/1, compare_RSS2_test2/1]).

-include_lib("xmerl/include/xmerl.hrl").
-import(xmerl_xpath, [string/2]).
-import(lists, [map/2]).

is_rss2_feed(File) ->
    {XML, _} = xmerl_scan:file(File),
    is_rss2_feed_sub_func(XML).

is_rss2_feed_sub_func(XML) ->
    Ver = xmerl_xpath:string("/rss[@version='2.0']/text()", XML),
    length(Ver) =/= 0. % если нашло хоть один version=2.0, то возвращаем true

get_feed_items(RSS2Feed) ->
    xmerl_xpath:string("//channel/item", RSS2Feed).

%функция тестирования получения элементов RSS
get_feed_items_test(File) ->
    {XML, _} = xmerl_scan:file(File),
    get_feed_items(XML).

get_item_time(Item) ->
    [Time] = xmerl_xpath:string("/item/pubDate/text()", Item),
    ND = httpd_util:convert_request_date(Time#xmlText.value),
    calendar:datetime_to_gregorian_seconds(ND).

%тест получения времени из RSS
get_item_time_test(File) -> 
    {XML, _} = xmerl_scan:file(File),
    map(fun get_item_time/1, get_feed_items(XML)).

% @private
% @doc Эта вспомогательная функция просматривает заданный XML элемент
%      и удаляет из него сведения о других XML элементах, например содержащиеся в полях
%      "parents" или "pos".
%
% @spec extract_xml(Node::xmlAny()) -> xmlAny()
%
extract_xml(Elem = #xmlElement{}) ->
    Elem#xmlElement{parents=[], pos=0,
        content=lists:map(fun extract_xml/1, Elem#xmlElement.content),
        attributes=lists:map(fun extract_xml/1, Elem#xmlElement.attributes)};
extract_xml(Attr = #xmlAttribute{}) ->
    Attr#xmlAttribute{parents=[], pos=0};
extract_xml(Text = #xmlText{}) ->
    Text#xmlText{parents=[], pos=0};
extract_xml(Comment = #xmlComment{}) ->
    Comment#xmlComment{parents=[], pos=0};
extract_xml(Other) ->
    Other.

collect_guid(Item) -> 
	Guid = xmerl_xpath:string("guid/text()", Item).

collect_title(Item) -> 
    Title = xmerl_xpath:string("title/text()", Item).

collect_link(Item) -> 
	Link = xmerl_xpath:string("link/text()", Item).

collect_pubdate(Item) -> 
	PubDate = xmerl_xpath:string("pubDate/text()", Item).

compare_RSS2_feed_items(Guid, Title, Link, PubDate, Guid, Title, Link, PubDate) ->
    same;
compare_RSS2_feed_items(Guid, _, _, _, Guid, _, _, _) ->
    updated;
compare_RSS2_feed_items(_, Title, _, _, _, Title, _, _) ->
    updated;
compare_RSS2_feed_items(_, _, Link, _, _, _, Link, _) ->
    updated;
compare_RSS2_feed_items(_, _, _, _, _, _, _, _) ->
    different.

compare_feed_items(ItemP1, ItemP2) ->
	Item1 = extract_xml(ItemP1),
	Item2 = extract_xml(ItemP2),
	Guid1 = collect_guid(Item1),
	Guid2 = collect_guid(Item2),
	Title1 = collect_title(Item1),
	Title2 = collect_title(Item2),
	Link1 = collect_link(Item1),
	Link2 = collect_link(Item2),
	PubDate1 = collect_pubdate(Item1),
	PubDate2 = collect_pubdate(Item2),
    compare_RSS2_feed_items(Guid1, Title1, Link1, PubDate1, Guid2, Title2, Link2, PubDate2).

compare_RSS2_test(File) ->
    {XML, _} = xmerl_scan:file(File),
    [X1, X2| _] = get_feed_items(XML),
    compare_feed_items(X1, X2).

compare_RSS2_test2(File) ->
    {XML, _} = xmerl_scan:file(File),
    [X1, X2| _] = get_feed_items(XML),
    compare_feed_items(X1, X1).