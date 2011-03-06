%% dims - The dimensions of the matrix
%% default - The value to use if a point does not exist
%% strict - Whether to strictly adhere to dimensions or allow any coordinates
%%   to be accessed
%% values - Values are in a modified triplet form: {{R,C}, V}
-record(sparse_matrix, {dims, default=0, strict=false, values=[]}).


