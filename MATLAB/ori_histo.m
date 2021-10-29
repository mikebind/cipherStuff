function [orihist, percenthist] = ori_histo(oriented_text)
% output should be 26 letters by 18 alphabets
[numrows,numalphs] = size(oriented_text);

alphabet = ['abcdefghijklmnopqrstuvwxyz'];

for i = 1:26
    orihist(i,:) = sum(lower(oriented_text)== alphabet(i));
end

numblanks = sum(~isletter(oriented_text));
for i = 1:26
    percenthist(i,:) = 100*orihist(i,:)./(numrows-numblanks);
end