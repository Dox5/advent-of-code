LB = 387638;
UB = 919123;
passwords = [];
for i = LB:UB
    list = str2double(regexp(num2str(i),'\d','match'));
    ascending = 0;
    pair = 0;
    num_list = [];
    for j = 1:length(list)-1
        if list(j) == list(j+1)
            num_list = find(list == list(j));
            if length(num_list) == 2 % remove if statement for first part
                pair = 1;
            end
            ascending = 1;
        elseif list(j) <= list(j+1)
            ascending = 1;
        elseif list(j) > list(j+1)
            ascending = 0;
            break
        end
    end
    if ascending == 1 && pair == 1
        passwords = [passwords;i];
    end
end