try
    load test_write_char.mat
    expdata = ['abcdefghijklmnopqrstuvwxyz';
               'ABCDEFGHIJKLMNOPQRSTUVWXYZ';
               '1234567890!@#$%^&*()-_=+`~';
               '[{]}\|;:''",<.>/?          '];
    pass = true;
    pass = pass && isequal(expdata,a);
catch me
    pass = false;
end
if pass
    fprintf('PASSED\n');
else
    fprintf('FAILED\n');
end
exit;
