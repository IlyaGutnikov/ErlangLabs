-module(rss_parse).

-export([is_rss2_feed/1]).
-export([get_feed_items_test/1, get_item_time_test/1]).

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
