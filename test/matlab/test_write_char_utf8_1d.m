try
    load test_write_char_utf8_1d.mat
    expdata = uint8([216,168,216,172,217,132,219,140,196,145,105,225,187,135,110])';
    expdata = native2unicode(expdata, 'UTF-8');
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
