try
    load test_write_empty_struct.mat
    expdata1 = repmat(struct,0,1);
    expdata2 = repmat(struct('field1',zeros(0,0),'field2',zeros(0,0)),0,1);
    expdata3 = struct('field1',zeros(0,1),'field2',zeros(0,1));
    expdata4(1).field1 = zeros(0,1);
    expdata4(1).field2 = repmat(' ',0,1);
    expdata4(2).field1 = repmat(struct,0,1);
    expdata4(2).field2 = repmat({zeros(0,0)},0,1);
    pass = true;
    pass = pass && isequal(var1,expdata1);
    pass = pass && isequal(var2,expdata2);
    pass = pass && isequal(var3,expdata3);
    pass = pass && isequal(var4,expdata4(:));
catch me
    pass = false;
end
if pass
    fprintf('PASSED\n');
else
    fprintf('FAILED\n');
end
exit;
