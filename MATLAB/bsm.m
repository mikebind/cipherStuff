function subsmat = bsm(subsmat,alphnum,letter,replacement)
% here, letter is ciphertext and replacement is plaintext

alphabet = ['abcdefghijklmnopqrstuvwxyz'];

column = find(alphabet==letter);
if subsmat(alphnum,column)~= blanks(1)
    yn = input('Overwrite previous guess?','s');
    if yn=='y'
        subsmat(alphnum,column) = replacement;
        disp('Overwritten')
    end
else
    subsmat(alphnum,column) = replacement;
end

return