try
    load test_write_struct_2d_logical.mat
    expdata(1).field1 = logical(mod(reshape(0:49,5,10),2));
    expdata(1).field2 = ~expdata(1).field1;
    expdata(2).field1 = false(0,5);
    expdata(2).field2 = tril(true(5));
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
