-module(billy_service).

-export([
	cont_create/0,
	cont_ls_ids/1,
	cont_rm_id/2,
	cont_set_id/3,
	cont_get_id/2,
	det_get_qua/1,
	det_set_qua/2,
	cont_is_empty/1
]).

-include("service.hrl").

-spec cont_create() -> {ok, svc_container()}.
-spec cont_ls_ids(svc_container()) -> {ok, [svc_type_id()]}.
-spec cont_rm_id(svc_container(), svc_type_id()) -> {ok, svc_container()}.
-spec cont_set_id(svc_container(), svc_type_id(), svc_details()) -> {ok, svc_container()}.
-spec cont_is_empty(svc_container()) -> boolean().
-spec cont_get_id(svc_container(), svc_type_id()) -> {ok, svc_details()} | not_present.

-spec det_get_qua(svc_details()) -> {ok, integer()}.
-spec det_set_qua(svc_details(), integer()) -> {ok, svc_details()}.

%%% API

cont_create() ->
	{ok, #svc_container{}}.

cont_ls_ids(#svc_container{ details = Details }) ->
	IDs = [ ID || { ID, _ } <- Details ],
	{ok, IDs}.

cont_rm_id(Container = #svc_container{ details = Details }, ID) ->
	DetailsNew = proplists:delete(ID, Details),
	{ok, Container#svc_container{
		details = DetailsNew
	}}.

cont_set_id(Container = #svc_container{ details = Details }, ID, D) ->
	DetailsNew = case proplists:is_defined(ID, Details) of
		true ->
			[ {ID, D} | proplists:delete(ID, Details) ];
		false ->
			[ {ID, D} | Details ]
	end,
	{ok, Container#svc_container{
		details = DetailsNew
	}}.

cont_get_id(_Container = #svc_container{ details = Details }, ID) ->
	case proplists:get_value(ID, Details, undefined) of
		undefined ->
			not_present;
		D ->
			{ok, D}
	end.


det_get_qua(#svc_details{ quantity = Q}) -> {ok, Q}.
det_set_qua(Details = #svc_details{}, Q) -> {ok, Details#svc_details{ quantity = Q }}.

cont_is_empty(#svc_container{
	details = Details
}) ->
	not details_is_any_not_empty(Details).


%%% Internal
details_is_any_not_empty([]) -> false;
details_is_any_not_empty([{_, #svc_details{
	quantity = Q
} } | SoFar]) ->
	case Q == 0 of
		true ->
			details_is_any_not_empty(SoFar);
		false ->
			true
	end.

