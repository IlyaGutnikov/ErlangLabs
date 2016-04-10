-module(rss_queue).

-include_lib("xmerl/include/xmerl.hrl").
-import(xmerl_xpath, [string/2]).

-compile(export_all).
-define(TIMEOUT,10000).
   
server(Q,Subs)->
  receive
    {add_item,RSSItem} ->
      NewQ = add_item_to_q(RSSItem,Q,Subs),
      server(NewQ,Subs);

    {get_all,ReqPid} ->
      ReqPid ! {self(),Q},
      server(Q,Subs);

    _Msg -> io:format("Unknown msg~p~n",[_Msg])  
  end.

start()-> Q = [],spawn(?MODULE,server,[Q,sets:new()]).

%start(Url)-> QPid = start() ,rss_reader:start(Url,QPid) ,QPid.

init([])-> start().

%init([Url])-> start(Url).

add_item(QPid,Item)->
  QPid ! {add_item,Item},
  ok.

add_item_to_q(NewItem,L1,[],Subs)->
  broadcast(NewItem,Subs)
  ,L1++[NewItem];

add_item_to_q(NewItem,L1,L=[OldItem|Rest],Subs)->
  case rss_parse:compare_feed_items(OldItem,NewItem) of
    same -> 
      L1++L ;
    updated -> 
       broadcast(NewItem,Subs), L1++Rest++[NewItem] ;
    different -> 
      add_item_to_q(NewItem,L1++[OldItem],Rest,Subs)
  end.
add_item_to_q(NewItem,Q,Subs)-> add_item_to_q(NewItem,[],Q,Subs).

add_feed(QPid,RSS2Feed)->
 Items=rss_parse:get_feed_items(RSS2Feed),
 [add_item(QPid,Item) || Item <- Items], 
 ok.

add_feed_from_file(QPid,File)->
 Items=rss_parse:get_feed_items_test(File),
 [add_item(QPid,Item) || Item <- Items], 
 ok.

get_all(QPid)->
  QPid!{get_all,self()}
  ,receive
    {QPid,Q} -> Q;
    _Msg -> {error,unknown_msg,_Msg}
   after 
    ?TIMEOUT -> {error,timeout}
  end.

broadcast(Item,PidSet)-> [ add_item(Pid,Item) || Pid <- sets:to_list(PidSet) ]. 