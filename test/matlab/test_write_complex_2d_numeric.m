try
    load 'test_write_complex_2d_numeric.mat'
    expdata = cast(reshape((1:50) + j*(51:100),5,10),type);
    pass = true;
    pass = pass && isa(a,class(expdata));
    pass = pass && all(size(a)==size(expdata));
    pass = pass && all(a(:)==expdata(:));
catch me
    pass = false;
end
if pass
    fprintf('PASSED\n');
else
    fprintf('FAILED\n');
end
exit;
