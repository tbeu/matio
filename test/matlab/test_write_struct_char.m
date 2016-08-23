try
    load test_write_struct_char.mat
    expdata(1).field1 = [];
    expdata(1).field2 = [];
    expdata(2).field1 = [];
    expdata(2).field2 = ['abcdefghijklmnopqrstuvwxyz';
               'ABCDEFGHIJKLMNOPQRSTUVWXYZ';
               '1234567890!@#$%^&*()-_=+`~';
               '[{]}\|;:''",<.>/?          '];
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
