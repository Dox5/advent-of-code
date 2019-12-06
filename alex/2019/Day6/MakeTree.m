function [outlist] = MakeTree(data,string,list)
%MAKETREE Summary of this function goes here
%   Detailed explanation goes here
    pos = find(contains(data, string));
    tmp = list;
    for i = pos
        pair = split(data(i), ')');
        if strcmp(pair(1),string)
            list_n = find(strcmp(tmp(1,:),pair{2}));
            list_c = find(strcmp(tmp(1,:),pair{1}));
            tmp(2,list_n) = str2double(tmp(2,list_c)) + 1; %#ok<FNDSB>
            tmp(3,list_n) = pair{1};
            tmp = MakeTree(data,pair{2},tmp);
        end
    end
    outlist = tmp;
end

