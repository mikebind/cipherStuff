function [subsMatOut, changeT] = updateFromSubTable(subsMat, subT)

alphabet = 'abcdefghijklmnopqrstuvwxyz';

change_idx = 0;
subsMatOut = subsMat; % copy
alph_num = [];
old = char([]);
new = char([]);
sub_idx = [];
for rIdx = 1:size(subT,1)
    row = subT(rIdx,:);
    alphNum = row.alph_num;
    plain_lett = row.plain_lett;
    lettCol = strfind(alphabet,row.ciph_lett);
    oldVal = subsMatOut(alphNum, lettCol);
    if oldVal~='_' && oldVal~=plain_lett
        % record change
        change_idx = change_idx+1;
        old(change_idx,1) = oldVal;
        new(change_idx,1) = plain_lett;
        alph_num(change_idx,1) = alphNum;
        sub_idx(change_idx, 1) = rIdx;
    end
    subsMatOut(alphNum, lettCol) = plain_lett;
end

changeT = table(alph_num, old, new, sub_idx);