-define(VERSION, element(2, application:get_key(ejabberd,vsn))).

-define(MYHOSTS, ejabberd_config:get_global_option(hosts)).
-define(MYNAME,  hd(ejabberd_config:get_global_option(hosts))).
-define(MYLANG,  ejabberd_config:get_global_option(language)).

-define(MSGS_DIR,    "msgs").
-define(CONFIG_PATH, "etc/ejabberd.cfg").
-define(LOG_PATH,    "log/ejabberd.log").

-define(EJABBERD_URI, "http://www.process-one.net/en/ejabberd/").

-define(S2STIMEOUT, 600000).

%%-define(DBGFSM, true).

%% ---------------------------------
%% Logging mechanism

%% Print in standard output
-define(PRINT(Format, Args),
    io:format(Format, Args)).

-define(DEBUG(Format, Args),
    lager:debug(Format, Args)).

-define(INFO_MSG(Format, Args),
    lager:info(Format, Args)).

-define(WARNING_MSG(Format, Args),
    lager:warning(Format, Args)).

-define(ERROR_MSG(Format, Args),
    lager:error(Format, Args)).

-define(CRITICAL_MSG(Format, Args),
    lager:critical(Format, Args)).

-record(session, {sid :: tuple(),
                  usr :: {binary(), binary(), binary()},
                  us :: {binary(), binary()},
                  priority :: integer(),
                  info :: list()}).

-ifdef(no_binary_to_integer).

-import(ejabberd_binary, [binary_to_integer/1,
                          integer_to_binary/1]).

-endif.
