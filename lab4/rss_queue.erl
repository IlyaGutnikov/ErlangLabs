-module(rss_queue).

-include_lib("xmerl/include/xmerl.hrl").
-import(xmerl_xpath, [string/2]).

% базовый аттерн для создания сервера в ерланге
-behaviour(gen_server).
-export([init/1]).

-export([init/1, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

-record(rssQueue,{queue,subscribers}).

start(Name) -> 
  gen_server:start({local, Name}, ?MODULE, [], []).
start(Name,Url)->
  gen_server:start({local, Name}, ?MODULE, [Url], []).

init([]) ->
  process_flag(trap_exit,true) 
  ,{ok, #rssQueue{queue=[],subscribers=sets:new()} };
init([Url]) -> 
  State = #rssQueue{queue=[],subscribers=sets:new()} 
  ,process_flag(trap_exit,true) 
  ,rss_reader:start(Url,self())
  ,{ok, State }.


handle_cast(_Msg={add_item,RSSItem=#xmlElement{name=item}}, State=#rssQueue{queue=Q,subscribers=Subs}) -> 
  NewQ = add_item_to_q(RSSItem,Q,Subs) 
  ,{noreply,State#rssQueue{queue=NewQ}};

handle_cast(_Msg={unsubscribe,QPid}, State=#rssQueue{subscribers=Subs}) -> 
  {noreply,State#rssQueue{subscribers=sets:del_element(QPid,Subs)}};

handle_cast(_Msg, State) -> {noreply, State}.

handle_info(_Info={'DOWN',_,_,QPid,_Reason},State=#rssQueue{subscribers=Subs})->
  {noreply, State#rssQueue{subscribers=sets:del_element(QPid,Subs)}};
handle_info(_Info={'EXIT',FromPid,_Reason},State)-> {noreply, State};
handle_info(_Info, State) -> {noreply, State}.

terminate(_Reason, _State) -> ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.