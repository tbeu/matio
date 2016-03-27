try
    load test_write_empty_cell.mat
    expdata1 = cell(0,1);
    expdata2 = {zeros(0,1);zeros(0,1)};
    pass = true;
    pass = pass && isequal(var1,expdata1);
    pass = pass && isequal(var2,expdata2);
catch me
    pass = false;
end
if pass
    fprintf('PASSED\n');
else
    fprintf('FAILED\n');
end
exit;
