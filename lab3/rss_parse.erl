-module(rss_parse).

-export([is_rss2_feed/1]).

is_rss2_feed(F) ->
    {XML, _} = xmerl_scan:file(F),
    is_rss2_feed_sub_func(XML).

is_rss2_feed_sub_func(XML) ->
    Ver = xmerl_xpath:string("/rss[@version='2.0']/text()", XML),
    length(Ver) =/= 0. % если нашло хоть один version=2.0, то возвращаем true