try
    load test_write_sparse.mat
    expdata = zeros(5,10);
    expdata(1:4:end,1:2:end) = 1;
    expdata(2:4,2:2:end) = 1;
    expdata = expdata.*reshape(1:50,5,10);
    pass = true;
    pass = pass && isa(sparse_matrix,class(expdata));
    pass = pass && issparse(sparse_matrix);
    pass = pass && all(size(sparse_matrix)==size(expdata));
    pass = pass && all(sparse_matrix(:)==expdata(:));
catch me
    pass = false;
end
if pass
    fprintf('PASSED\n');
else
    fprintf('FAILED\n');
end
exit;
