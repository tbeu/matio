try
    load test_write_char_utf8.mat
    expdata = uint8([216,168,196,145,216,172,105,217,132,225,187,135,219,140,110]);
    expdata = reshape(native2unicode(expdata, 'UTF-8'), 2, 4);
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
