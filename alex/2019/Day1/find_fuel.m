function fuel = find_fuel(weight)
    stage1 = floor(weight/3);
    stage2 = stage1 - 2;
    if stage2 <= 0
        fuel = weight;
    else
        fuel = weight + find_fuel(stage2);
    end
end