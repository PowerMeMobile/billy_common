-ifndef(__BILLY_SESSION_PIQI_HRL__).
-define(__BILLY_SESSION_PIQI_HRL__, 1).

-type(billy_session_pdu() :: 
      {hello, billy_session_hello()}
    | {bye, billy_session_bye()}
    | {bind_request, billy_session_bind_request()}
    | {bind_response, billy_session_bind_response()}
    | {require_unbind, billy_session_require_unbind()}
    | {unbind_request, billy_session_unbind_request()}
    | {unbind_response, billy_session_unbind_response()}
).
-type(billy_session_session_state_name() :: 
      st_negotiating
    | st_unbound
    | st_binding
    | st_bound
    | st_unbinding
).
-record(billy_session_hello, {
    session_id :: binary(),
    bind_request_timeout :: integer(),
    server_version :: binary()
}).
-record(billy_session_bye, {
    state_name :: billy_session_session_state_name(),
    reason :: billy_session_bye_reason(),
    reason_long :: binary()
}).
-type(billy_session_bye_reason() :: 
      normal
    | internal_error
    | protocol_error
    | {custom, binary()}
).
-record(billy_session_bind_request, {
    client_id :: binary(),
    client_pw :: binary()
}).
-record(billy_session_bind_response, {
    result :: billy_session_bind_result()
}).
-type(billy_session_bind_result() :: 
      accept
    | {reject, binary()}
).
-record(billy_session_require_unbind, {
    timeout :: integer(),
    reason :: billy_session_require_unbind_reason()
}).
-type(billy_session_require_unbind_reason() :: 
      {custom, binary()}
).
-record(billy_session_unbind_request, {
    reason :: billy_session_unbind_reason()
}).
-type(billy_session_unbind_reason() :: 
      normal
    | internal_error
    | required_by_server
    | {custom, binary()}
).
-record(billy_session_unbind_response, {
    bind_request_timeout :: integer()
}).

-type(billy_session_hello() :: #billy_session_hello{}).
-type(billy_session_bye() :: #billy_session_bye{}).
-type(billy_session_bind_request() :: #billy_session_bind_request{}).
-type(billy_session_bind_response() :: #billy_session_bind_response{}).
-type(billy_session_require_unbind() :: #billy_session_require_unbind{}).
-type(billy_session_unbind_request() :: #billy_session_unbind_request{}).
-type(billy_session_unbind_response() :: #billy_session_unbind_response{}).


-endif.
