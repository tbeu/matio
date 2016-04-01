try
    load test_write_struct_2d_numeric.mat
    expdata(1).field1 = cast(reshape((1:12),3,4),type);
    expdata(1).field2 = cast(reshape((13:24),3,4),type);
    expdata(2).field1 = cast(reshape((25:36),3,4),type);
    expdata(2).field2 = cast(reshape((37:48),3,4),type);
    pass = true;
    pass = pass && isequal(a,expdata(:));
    pass = pass && strcmp(class(a(1).field1),class(expdata(1).field1));
    pass = pass && strcmp(class(a(1).field2),class(expdata(1).field2));
    pass = pass && strcmp(class(a(2).field1),class(expdata(2).field1));
    pass = pass && strcmp(class(a(2).field2),class(expdata(2).field2));
catch me
    pass = false;
end
if pass
    fprintf('PASSED\n');
else
    fprintf('FAILED\n');
end
exit;
