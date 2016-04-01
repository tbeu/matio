try
    load test_write_cell_complex_2d_numeric.mat
    expdata = {cast(reshape((1:12)+j*(51:62),3,4),type);...
               cast(reshape((13:24)+j*(63:74),3,4),type);...
               cast(reshape((25:36)+j*(75:86),3,4),type);...
               cast(reshape((37:48)+j*(87:98),3,4),type);};
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
