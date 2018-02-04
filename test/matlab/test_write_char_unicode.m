try
    load test_write_char_unicode.mat
    expdata = char([1576, 1580, 1604, 1740; 273, 105, 7879, 110]);
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
