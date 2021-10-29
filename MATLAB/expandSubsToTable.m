function [subT,subs] = expandSubsToTable(subs,frame)

% expanded version of subs should be a table with the following columns
% nsnp_idx, alphabet number, ciph_lett, plain_lett, sub_idx, sub_ciph,
% sub_plain

row_idx = 1; 
for sub_idx = 1:length(subs)
    sub = subs{sub_idx};
    nsnp_start = sub{1};
    plain = sub{2};
    ciph = frame.nsnpl(nsnp_start:nsnp_start+length(plain)-1);
    for plain_idx = 1:length(plain)
        cur_nsnp_idx = nsnp_start+plain_idx-1;
        cur_alph_num = frame.nsnp_alpha_nums(cur_nsnp_idx);
        cur_ciph_lett = ciph(plain_idx);
        cur_plain_lett = plain(plain_idx);
        % Add to column variables
        nsnp_idx(row_idx,1) = cur_nsnp_idx;
        alph_num(row_idx,1) = cur_alph_num;
        ciph_lett(row_idx,1) = cur_ciph_lett;
        plain_lett(row_idx,1) = cur_plain_lett;
        subs_idx(row_idx,1) = sub_idx;
        sub_ciph{row_idx,1} = ciph;
        sub_plain{row_idx,1} = plain;
        % Increment
        row_idx = row_idx+1; 
    end    
    % Add nsnpl ciphertext to subs 
    subs{sub_idx}{3} = ciph;
end
% Build table from columns
subT = table(nsnp_idx, alph_num, ciph_lett, plain_lett, subs_idx, sub_ciph, sub_plain);

