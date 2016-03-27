try
    load test_write_cell_2d_logical.mat
    expdata = {triu(true(5));tril(true(5));logical(eye(5));false(0,5)};
    pass = true;
    pass = pass && isequal(a,expdata(:));
    pass = pass && strcmp(class(a{1}),class(expdata{1}));
    pass = pass && strcmp(class(a{2}),class(expdata{2}));
    pass = pass && strcmp(class(a{3}),class(expdata{3}));
    pass = pass && strcmp(class(a{4}),class(expdata{4}));
catch me
    pass = false;
end
if pass
    fprintf('PASSED\n');
else
    fprintf('FAILED\n');
end
exit;
