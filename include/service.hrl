
-include_lib("billy_common/include/billy_session_piqi.hrl").

-record(svc_details, {
	quantity = 0 :: integer()
}).

-type svc_type_id() :: string().
-type svc_details() :: #svc_details{}.

-record(svc_container, {
	details = [] :: [{ svc_type_id(), svc_details() }]
}).

-type svc_container() :: #svc_container{}.


