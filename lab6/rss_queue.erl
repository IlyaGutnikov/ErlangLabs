-module(rss_queue).
-include("logging.hrl").
-compile(export_all).
-define(TIMEOUT,10000).

init([])->
  start();
init([Url])->
  start(Url).

%% @doc Функция должна возвращать PID созданного процесса
start()->
  Q = [],
  spawn(?MODULE,server,[Q,sets:new()]).

start(Url)->
  QPid = start(),
  rss_reader:start(Url,QPid),
  QPid.

%% @doc реализован цикл сервера процесса очереди RSS   
server(Q,Subs)->
  receive

%% @doc регистрация новых элементов в очереди
    {add_item,RSSItem} ->
      NewQ = add_item_to_q(RSSItem,Q,Subs) 
      ,server(NewQ,Subs);

%% @doc Другой процесс может отправить это сообщение очереди RSS,
%% чтобы получить все содержимое очереди.
    {get_all,ReqPid} ->
      ReqPid ! {self(),Q}
      ,?INFO("Sent rss items to ~p~n",[ReqPid]) 
      ,server(Q,Subs);

%% @doc удаляет заданный PID из множества подписчиков
    {unsubscribe,QPid} ->
      server(Q,sets:del_element(QPid,Subs));

%% @doc позволяет очереди подписаться на получение элементов ленты другой очереди
    {subscribe,QPid} ->
      erlang:monitor(process,QPid)
      ,?INFO("New subscriber ~p to ~p~n",[QPid,self()]) 
      ,[add_item(QPid,Item) || Item <- Q]
      ,server(Q,sets:add_element(QPid,Subs));
    {'DOWN',_,_,QPid,_Reason}->
      server(Q,sets:del_element(QPid,Subs));
    _Msg -> io:format("Unknown msg~p~n",[_Msg])  
  end.

%% @doc упрощает процедуру отправки элемента в очередь
add_item(QPid,Item)->
  QPid ! {add_item,Item}
  ,ok.

%% @doc Эта функция должна извлекать все элементы из документа ленты, 
%% и отправлять все элементы по порядку в очередь
add_feed(QPid,RSS2Feed) when is_pid(QPid) ->
 Items=rss_parse:get_feed_items(RSS2Feed)
 ,[add_item(QPid,Item) || Item <- Items]
 ,?INFO("Added N=~p items from the feed to ~p ~n",[length(Items),QPid]) 
 , ok.

%% @doc упрощает процедуру получения списка элементов ленты от процесса
get_all(QPid) when is_pid(QPid)->
  QPid!{get_all,self()}
  ,receive
    {QPid,Q} -> Q ;
    _Msg -> {error,unknown_msg,_Msg}
   after 
    ?TIMEOUT -> {error,timeout}
  end.

%% @doc передача объекта нескольким пидам
broadcast(Item,PidSet)->
  [ add_item(Pid,Item) || Pid <- sets:to_list(PidSet) ].

%% @doc добавление нового элемента в очередь
add_item_to_q(NewItem,L1,[],Subs)->
  ?INFO("New item ~p ~n",[self()])
  ,broadcast(NewItem,Subs)
  ,L1++[NewItem];
%% @doc обновление элемента
add_item_to_q(NewItem,L1,L=[OldItem|Rest],Subs)->
  case rss_parse:compare_feed_items(OldItem,NewItem) of
    same -> 
      L1++L ;
    updated -> 
      ?INFO("Updated item ~p ~n",[self()])
      ,broadcast(NewItem,Subs)
      ,L1++Rest++[NewItem] ;
    different -> 
      add_item_to_q(NewItem,L1++[OldItem],Rest,Subs)
  end.
  
add_item_to_q(NewItem,Q,Subs)->
  add_item_to_q(NewItem,[],Q,Subs).