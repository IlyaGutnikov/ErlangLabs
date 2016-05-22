-module(rss_queue).
-include("logging.hrl").
-compile(export_all).
-define(TIMEOUT,10000).
-include_lib("xmerl/include/xmerl.hrl").

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3, get_feed_xml/2]).

-export([start/1]).

-record(rssQ,{queue,subscribers}).

start(Name) -> 
  gen_server:start({local, Name}, ?MODULE, [], []).
start(Name,Url)->
  gen_server:start({local, Name}, ?MODULE, [Url], []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc init
%% @doc В случае если gen_server процесс запущен 
%% с использованием функций gen_server:start/3,4 
%% или gen_server:start_link/3.4, эта функция будет 
%% вызвана созданным процессом для проведения инициализации.

init([]) ->
  process_flag(trap_exit,true),
  {ok, #rssQ{queue=[],subscribers=sets:new()} };


init([Url]) -> 
  State = #rssQ{queue=[],subscribers=sets:new()},
  process_flag(trap_exit,true),
  rss_reader:start(Url,self()),
  {ok, State }.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc handle_call
%% @doc Когда gen_server процесс получает запрос 
%% посланный при помощи gen_server:call/2,3 
%% или gen_server:multi_call/2,3,4, для обработки 
%% запроса вызывается эта функция.

handle_call(_Request={subscribe,QPid}, _From, State=#rssQ{queue=Q,subscribers=Subs}) ->
  {Reply,NewState} = case sets:is_element(QPid,Subs) of
    true -> {{error,already_subscribed},State};
    false ->  erlang:monitor(process,QPid),
    ?INFO("New subscriber ~p to ~p~n",[QPid,self()]),
    [add_item(QPid,Item) || Item <- Q],
    {ok,State#rssQ{subscribers=sets:add_element(QPid,Subs)}}
  end,
  {reply,Reply, NewState};

handle_call(_Request={get_all}, _From, State=#rssQ{queue=Q}) -> 
  {reply,Q,State};
handle_call(_Request, _From, State) -> 
  {reply,{error,{unknown_request,_Request}}, State}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc handle_cast
%% @doc Эта функция вызывается в тот момент когда gen_server 
%% процесс получает запрос, посланый с использованием 
%% функции gen_server:cast/2 или gen_server:abcast/2,3.

handle_cast(_Msg={add_item,RSSItem=#xmlElement{name=item}}, State=#rssQ{queue=Q,subscribers=Subs}) -> 
  NewQ = add_item_to_q(RSSItem,Q,Subs),
  {noreply,State#rssQ{queue=NewQ}};

handle_cast(_Msg={unsubscribe,QPid}, State=#rssQ{subscribers=Subs}) -> 
  {noreply,State#rssQ{subscribers=sets:del_element(QPid,Subs)}};

handle_cast(_Msg, State) -> 
  ?WARN("Unknown msg {~p} to Q{~p}",[_Msg,State]),
  {noreply, State}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc handle_info
%% @doc Эта функция вызывается когда происходит таймаут ожидания 
%% сообщений на gen_server процессе или когда приходит любое сообщение 
%% отличное от асинхронного, синхронного запросов.

handle_info(_Info={'DOWN',_,_,QPid,_Reason},State=#rssQ{subscribers=Subs})->
  {noreply, State#rssQ{subscribers=sets:del_element(QPid,Subs)}};

handle_info(_Info={'EXIT',FromPid,_Reason},State)->
  ?ERROR("RSS Reader ~p died for ~p with reason ~n",[FromPid,self(),_Reason]),
  {noreply, State};

handle_info(_Info, State) -> 
  {noreply, State}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc terminate
%% @doc Эта функция вызывается перед завершением gen_server процесса 
%% и является противоположностью функции Module:init/1, выполняя необходимую очистку. 
%% Как только происходит возврат контроля в gen_server процесс, он завершается с причиной 
%% Reason. Возвращаемое значение этой функции игнорируется.

terminate(_Reason, _State) -> ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc code_change
%% @doc Эта функция вызывается когда происходит 
%% обновление/откат версии gen_server процесса 
%% и ему необходимо обновить свое внутреннее состояние, 
%% для соответстия реалиям работающего кода

code_change(_OldVsn, State, _Extra) -> {ok, State}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc Вспомогательные функции
%% @doc Эти функции подписывают очередь Queue1 на сообщения очереди Queue2.
subscribe(From,To)->
  gen_server:call(To,{subscribe,From}).

%% @doc добавление нового элемента в очередь  
add_item_to_q(NewItem,Q,Subs)->
  add_item_to_q(NewItem,[],Q,Subs).

add_item_to_q(NewItem,L1,[],Subs)->
  ?INFO("New item ~p ~n",[self()]),
  broadcast(NewItem,Subs),
  L1++[NewItem];

add_item_to_q(NewItem,L1,L=[OldItem|Rest],Subs)->
  case rss_parse:compare_feed_items(OldItem,NewItem) of
    same -> 
      L1++L ;
    updated -> 
      ?INFO("Updated item ~p ~n",[self()]),
      broadcast(NewItem,Subs),
      L1++Rest++[NewItem] ;
    different -> 
      add_item_to_q(NewItem,L1++[OldItem],Rest,Subs)
  end.

%% @doc упрощает процедуру отправки элемента в очередь 
add_item(QPid,Item)->
  ok = gen_server:cast(QPid , {add_item,Item} ),ok.

%% @doc Эта функция должна извлекать все элементы из документа ленты, 
%% и отправлять все элементы по порядку в очередь
add_feed(QPid,RSS2Feed)->
 Items=rss_parse:get_feed_items(RSS2Feed),
 [add_item(QPid,Item) || Item <- Items],
 ?INFO("Added N=~p items from the feed to ~p ~n",[length(Items),QPid]),
 ok. 

%% @doc упрощает процедуру получения списка элементов ленты от процесса
get_all(QPid)->
  gen_server:call(QPid,{get_all}).

%% @doc передача объекта нескольким пидам
broadcast(Item,PidSet)->
  [ add_item(Pid,Item) || Pid <- sets:to_list(PidSet) ]. 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Лабораторная работа №7
%% @doc получаем список элементов RSS
get_feed_xml(QueueName, RequestURL) -> 
  QPid = whereis(QueueName),
  {ok, RSSItems} = rss_queue:get_all(QPid),
  RSSItemXML = lists:flatten(xmerl:export_content(RSSItems, xmerl_xml)),
  LinkToURL = io_lib:format("<link>~s</link>\n", [RequestURL]),
  if 
    is_atom(QueueName) == true ->
      TitleTag = io_lib:format("<title>~s</title>\n", [QueueName]),
      DescriptionTag = io_lib:format("<description>Aggregated feed queue from ~s</description>\n", [QueueName]);
    true ->
      TitleTag = io_lib:format("<title>Unknown queue</title>\n", []),
      DescriptionTag = io_lib:format("<description>Aggregated feed queue</description>\n", [])
  end,
  RSSResponse = 
    ["<?xml version=\"1.0\"?>\n"
      "<rss xmlns:media=\"http://search.yahoo.com/mrss/\""
          " xmlns:feedburner=\"http://rssnamespace.org/feedburner/ext/1.0\""
          " xmlns:digg=\"http://digg.com/docs/diggrss/\""
          " xmlns:dc=\"http://purl.org/dc/elements/1.1/\""
          " version=\"2.0\">\n"
          "<channel>\n" ++
            TitleTag ++
              DescriptionTag ++
              LinkToURL ++
              RSSItemXML ++
            "</channel>\n"
          "</rss>\n"
    ].
