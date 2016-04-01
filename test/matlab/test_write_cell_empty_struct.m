try
    load test_write_cell_empty_struct.mat
    expdata{1,1} = struct('field1',[51.,52.;53.,54.],'field2',[],'field3',[]);
    expdata{1,2} = expdata{1,1};
    expdata{1,3} = expdata{1,1};
    pass = isequal(var1,expdata1);
catch me
    pass = false;
end
if pass
    fprintf('PASSED\n');
else
    fprintf('FAILED\n');
end
exit;
