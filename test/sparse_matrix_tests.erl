-module(sparse_matrix_tests).
-include_lib("eunit/include/eunit.hrl").

from_triplet_test_() ->
  Raw = [ {new_york,montreal,8}, {new_york,boston,4}, {new_york,buffalo,9},
    {buffalo,toronto,3}, {boston,montreal,7} ],
  Mat = sparse_matrix:from_triplet(Raw),
  ?_assertMatch({3,4}, sparse_matrix:dimensions(Mat)),
  ?_assertMatch(4, sparse_matrix:value({new_york,boston}, Mat)),
  ?_assertMatch(0, sparse_matrix:value({philadelphia,boston}, Mat)).

from_triplet_default_test_() ->
  Raw = [ {new_york,montreal,8}, {new_york,boston,4}, {new_york,buffalo,9},
    {buffalo,toronto,3}, {boston,montreal,7} ],
  Mat = sparse_matrix:from_triplet(Raw, -1),
  ?_assertMatch({3,4}, sparse_matrix:dimensions(Mat)),
  ?_assertMatch(4, sparse_matrix:value({new_york,boston}, Mat)),
  ?_assertMatch(-1, sparse_matrix:value({philadelphia,boston}, Mat)).

integer_index_test_() ->
  Raw = [ {1,1, 5}, {1,2, 3}, {2,1, 4}, {2,6, 3} ],
  Mat = sparse_matrix:from_triplet(Raw),
  ?_assertMatch({2,6}, sparse_matrix:dimensions(Mat)),
  ?_assertMatch(3, sparse_matrix:value({1,2}, Mat)),
  ?_assertMatch(0, sparse_matrix:value({1,5}, Mat)).

addition_test_() ->
  Raw1 = [ {new_york,montreal,8}, {new_york,boston,4}, {new_york,buffalo,9},
    {buffalo,toronto,3}, {boston,montreal,7} ],
  Raw2 = [ {albany,pittsburg,8}, {new_york,boston,5}, {new_york,toronto,11},
    {buffalo,toronto,6}, {boston,buffalo,11} ],
  Mat1 = sparse_matrix:from_triplet(Raw1),
  Mat2 = sparse_matrix:from_triplet(Raw2),
  Sum = sparse_matrix:add(Mat1,Mat2),
  ?_assertMatch(8, sparse_matrix:value({new_york,montreal}, Sum)),
  ?_assertMatch(9, sparse_matrix:value({new_york,boston}, Sum)),
  ?_assertMatch(9, sparse_matrix:value({buffalo,toronto}, Sum)),
  ?_assertMatch(8, sparse_matrix:value({albany,pittsburg}, Sum)),
  ?_assertMatch(11, sparse_matrix:value({new_york,toronto}, Sum)).

