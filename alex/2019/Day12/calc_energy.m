function [varargout] = calc_energy(data)
%CALC_ENERGY Summary of this function goes here
%   Detailed explanation goes here
    pot = [];
    kin = [];
    for i = 1:length(data)
        pot = [pot;sum(abs(data(i,:,1)))];
        kin = [kin;sum(abs(data(i,:,2)))];
    end
    if nargout == 1
        varargout{1} = sum(pot.*kin);
    elseif nargout == 2
        varargout{1} = sum(pot.*kin);
        varargout{2} = sum(pot);
    elseif nargout == 3
        varargout{1} = sum(pot.*kin);
        varargout{2} = sum(pot);
        varargout{3} = sum(kin);
    end
end

