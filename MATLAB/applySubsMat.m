function tr = applySubsMat(subsMat, frame)
% Funcion to apply substitution matrix to a given frame.
% Especially want to output original frame and nsnp frames with
% substitutions filled in

nsnpl = frame.nsnpl;
nsnp_alpha_nums = frame.nsnp_alpha_nums;

alphabet = 'abcdefghijklmnopqrstuvwxyz';

% Translate
tnsnpl = repmat('_',size(nsnpl));
% Loop over alphabet numbers
numAlphas = size(subsMat,1);
for alphNum = 1:numAlphas
    for lett_idx = 1:26
        lett = alphabet(lett_idx);
        mask = nsnp_alpha_nums==alphNum & nsnpl==lett;
        tnsnpl(mask)= subsMat(alphNum, lett_idx);
    end
end

% Two possible approaches:
% 1. translate nsnpl first and then fill other representations from there
% 2. translate each based on their alpha_nums
tr.nsnpl = tnsnpl;

tr.orig = frame.orig;
tr.orig(frame.nsnp_idxs_in_orig_frame>0) = tr.nsnpl;
upper_mask = frame.orig~=lower(frame.orig);
tr.orig(upper_mask) = upper(tr.orig(upper_mask));

% Build interlaced version by alternating sections before newlines
tr.interlaced=[];
lf = strfind(frame.orig, char(10));
starts = [1,lf(1:end-1)+1];
fins = lf;
for idx = 1:length(starts)
    tr.interlaced = [tr.interlaced, tr.orig(starts(idx):fins(idx)),frame.orig(starts(idx):fins(idx))];
end


% Make a version with / at spaces (to more easily see word boundaries
tr.oslash = tr.orig;
tr.oslash(tr.orig==' ') = '/';
% Let's insert a # at step boundaries!
for idx = 1:length(frame.step_boundaries)
    bnd_idx = length(frame.step_boundaries)-idx+1;
    bnd_nsnp = frame.step_boundaries(bnd_idx);
    txt = sprintf('#%i(%i)#',bnd_idx, bnd_nsnp);
    bnd_orig = find(frame.nsnp_idxs_in_orig_frame==bnd_nsnp);
    % Insert
    tr.oslash = [tr.oslash(1:bnd_orig-1),txt,tr.oslash(bnd_orig:end)];
    
end