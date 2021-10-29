function invAlph = invertAlph(alph)
% If alph is plain letters to be replacing cipher letters implicitly
% running in normal order, invAlph is cipher letters to be replaced by
% plain letters implicitly running in normal order

% NOTE that this only works with complete alphabets with all 26 unique
% letters

reg_alph = 'abcdefghijklmnopqrstuvwxyz';

[~,srtOrder] = sort(alph);
invAlph = reg_alph(srtOrder);