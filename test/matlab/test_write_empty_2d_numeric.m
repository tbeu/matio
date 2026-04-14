try
    load test_write_empty_2d_numeric.mat
    expdata = zeros(0,10);
    pass = true;
    pass = pass && isa(empty,class(expdata));
    pass = pass && isempty(empty);
    pass = pass && all(size(empty)==size(expdata));
    pass = pass && all(empty(:)==expdata(:));
catch me
    pass = false;
end
if pass
    fprintf('PASSED\n');
else
    fprintf('FAILED\n');
end
exit;
