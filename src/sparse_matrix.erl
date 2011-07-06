-module(sparse_matrix).
-export([new/0, new/1, new/2,
  from_triplet/1, from_triplet/2,
  from_list/1, from_list/2, from_list/3,
  get/2, put/2, delete/2, coordinates/1, dimensions/1,
  add/2, mult/2 ]).
-include("sparse_matrix.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% CONSTRUCTORS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
new() -> #sparse_matrix{}.
new(Default) -> #sparse_matrix{default=Default}.
new(Default,Boolean) when is_boolean(Boolean) -> 
  #sparse_matrix{default=Default, symmetric=Boolean}.


from_triplet(Triplets) ->
	from_list([ {{R,C},V} || {R,C,V} <- Triplets]).	

%% @spec from_list(List,[{symmetric,false},{dims,{R,C}},{strict,false},
%%                        {default,0}]) -> #sparse_matrix{}
from_triplet(Triplets,Options) when is_list(Options)-> 
	from_list([ {{R,C},V} || {R,C,V} <- Triplets ], Options);

%% @deprecated
from_triplet(Triplets, Default) ->
  from_list([ {{R,C},V} || {R,C,V} <- Triplets ], Default).


from_list(List) -> from_list(List, [{default,0},{symmetric,false}]).

%% @spec from_list(List,[{symmetric,false},{dims,{R,C}},{strict,false},
%%                        {default,0}]) -> #sparse_matrix{}
from_list(List,Options) when is_list(Options)->
	
	DimsTest = proplists:get_value(dims, Options),
	Dims =
	case DimsTest of
		{R,C} when is_integer(R) and is_integer(C) -> DimsTest;
		_ ->
			MaxR = max_rows(List, hd(List)),
			MaxC = max_cols(List, hd(List)),
			{MaxR, MaxC}
	end,
	
	Default = proplists:get_value(default, Options,0),
	
	SymmetricTest = proplists:get_value(symmetric, Options,false),
	Symmetric = if is_boolean(SymmetricTest) -> SymmetricTest; 
							true-> false end,
	Strict =false,
	MatrixT = #sparse_matrix{ dims=Dims, default=Default, strict=Strict,
														values=List, symmetric=Symmetric},							 	
	Matrix = enforce_upper(MatrixT),
  Matrix;

%% @deprecated
from_list(List, Default) ->
  MaxR = max_rows(List, hd(List)),
  MaxC = max_cols(List, hd(List)),
  from_list(List, Default, {MaxR,MaxC}).

%% @deprecated
from_list(List, Default, {MaxR,MaxC}) when is_integer(MaxR), is_integer(MaxC) ->
  #sparse_matrix{dims={MaxR,MaxC}, default=Default, values=List}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% ACCESSORS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Get the get of a cell in the matrix
get({R,C}, #sparse_matrix{symmetric=false}=Matrix) ->
	El = get_value({R,C}, Matrix#sparse_matrix.values),
  get(El, Matrix#sparse_matrix.default);

get({R,C}, #sparse_matrix{symmetric=true}=Matrix) ->
	El = get_value({R,C}, Matrix#sparse_matrix.values,upper),
	get(El, Matrix#sparse_matrix.default);

get([], Default) -> Default;
get([H|_], _) -> H.

    
%% Put a value into the matrix using triplet form. This replaces any existing
%% value
%% TODO Add a dimensional check
put({R,C,V}, #sparse_matrix{symmetric=false}=Matrix) ->  
	Values = put_value({R,C,V}, Matrix#sparse_matrix.values),
  Matrix#sparse_matrix{values=Values};

put({R,C,V}, #sparse_matrix{symmetric=true}=Matrix) ->
	Values = put_value({R,C,V}, Matrix#sparse_matrix.values,upper),
	Matrix#sparse_matrix{values=Values}.


delete({R,C}, #sparse_matrix{symmetric=false}=Matrix) ->
  Values = delete_value({R,C},Matrix#sparse_matrix.values),
	Matrix#sparse_matrix{values=Values};

delete({R,C}, #sparse_matrix{symmetric=true}=Matrix) ->
	Values = delete_value({R,C}, Matrix#sparse_matrix.values,upper),	
	Matrix#sparse_matrix{values=Values}.
 
coordinates(Matrix) when is_record(Matrix, sparse_matrix) ->
  [ {R,C} || {{R,C}, _} <- Matrix#sparse_matrix.values ].

dimensions(Matrix) when is_record(Matrix, sparse_matrix) ->
  Matrix#sparse_matrix.dims.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% OPERATIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
put_value({R,C,V},Values) ->
  lists:keystore({R,C}, 1, Values, {{R,C},V}).

put_value({R,C,V},Values,upper) ->
	if R > C -> put_value({C,R,V},Values);
	true -> put_value({R,C,V},Values)
	end.

add(A,B) when is_record(A,sparse_matrix), is_record(B,sparse_matrix) ->
  ASet = ordsets:from_list(coordinates(A)),
  BSet = ordsets:from_list(coordinates(B)),
  Intersection = ordsets:intersection(ASet,BSet),
  Fn = fun(Coords) -> {Coords, get(Coords,A) + get(Coords,B)} end,

  SubFn = fun(Set, Int, Matrix) ->
    [ {Tuple, V} || {Tuple, V} <- Matrix#sparse_matrix.values, 
      Unique <- ordsets:to_list(ordsets:subtract(Set, Int)), Tuple == Unique ]
  end,
  Values = lists:map(Fn, Intersection)
    ++ SubFn(ASet,Intersection,A) ++ SubFn(BSet,Intersection,B),
  from_list(Values).
  
mult(A, B) when is_record(A,sparse_matrix), is_record(B,sparse_matrix) ->
  not_implemented.
  
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% PRIVATE %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
delete_value({R,C},Values) ->
  lists:keydelete({R,C}, 1, Values).
delete_value({R,C},Values,upper) ->
	if R > C -> delete_value({C,R},Values);
	true -> delete_value({R,C},Values)
	end.

get_value({R,C},Values) ->
   [Value || {{R1,C1},Value} <- Values, R1==R, C1==C].
get_value({R,C},Values,upper) ->	
	if R > C -> get_value({C,R},Values); 
	true -> get_value({R,C},Values)
	end.

%enforce upper triangular form for symmetric matrices
enforce_upper(#sparse_matrix{symmetric=false}=Matrix) ->
 Matrix;
enforce_upper(#sparse_matrix{symmetric=true}=Matrix) ->
  Values = Matrix#sparse_matrix.values,	
	NewValues =
		lists:map(fun({{R,C},V}) ->
					   if R > C -> {{C,R},V};
						 true -> {{R,C},V}
						 end								 
						end,Values),
	Matrix#sparse_matrix{values=NewValues}.

%% Get the number of rows in the matrix
max_rows(List, {{R0,_},_}) when is_integer(R0) ->
  hd(lists:reverse(lists:sort([R || {{R,_},_} <- List])));
max_rows(List, _) ->
  length(lists:usort([R || {{R,_},_} <- List])).

%% Get the number of columns in the matrix
max_cols(List, {{_,C0},_}) when is_integer(C0) ->
  hd(lists:reverse(lists:sort([C || {{_,C},_} <- List])));
max_cols(List, _) ->
  length(lists:usort([C || {{_,C},_} <- List])).
  
