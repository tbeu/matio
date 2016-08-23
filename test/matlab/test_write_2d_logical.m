try
    load test_write_2d_logical.mat
    expdata.l0 = false(0,10);
    expdata.l1 = logical(mod(reshape(0:49,5,10),2));
    pass = true;
    pass = pass && isa(l0,class(expdata.l0));
    pass = pass && all(size(l0)==size(expdata.l0));
    pass = pass && all(l0(:)==expdata.l0(:));
    pass = pass && isa(l1,class(expdata.l1));
    pass = pass && all(size(l1)==size(expdata.l1));
    pass = pass && all(l1(:)==expdata.l1(:));
    pass = pass && isa(l2,class(expdata.l1));
    pass = pass && all(size(l2)==size(expdata.l1));
    pass = pass && all(l2(:)==expdata.l1(:));
    pass = pass && isa(l4,class(expdata.l1));
    pass = pass && all(size(l4)==size(expdata.l1));
    pass = pass && all(l4(:)==expdata.l1(:));
    if exist('l8','var')
        pass = pass && isa(l8,class(expdata.l1));
        pass = pass && all(size(l8)==size(expdata.l1));
        pass = pass && all(l8(:)==expdata.l1(:));
    end
catch me
    pass = false;
end
if pass
    fprintf('PASSED\n');
else
    fprintf('FAILED\n');
end
exit;
