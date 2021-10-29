function tr = apply3dSubsMat(sm3d, frame)
% Function to apply a 3d substitution matrix to a given frame
% The produced translation should have the first layer translation on the
% top line, followed by as many lines as there are layers to the sub matrix
% with the other options identified listed on the additional lines


nsnpl = frame.nsnpl;
nsnp_alpha_nums = frame.nsnp_alpha_nums;

alphabet = 'abcdefghijklmnopqrstuvwxyz';

% Translate
numLayers = size(sm3d,3);
tnsnpl = repmat('_',numLayers, size(nsnpl,2)); % multiline
% Loop over alphabet numbers and layers
numAlphas = size(sm3d,1);
for alphNum = 1:numAlphas
    for lett_idx = 1:26
        lett = alphabet(lett_idx);
        mask = nsnp_alpha_nums==alphNum & nsnpl==lett;
        % Loop over layers
        for layerIdx = 1:numLayers
            tnsnpl(layerIdx, mask)= sm3d(alphNum, lett_idx, layerIdx);
        end
    end
end

tr.nsnpl = tnsnpl;

% To build interlaced lines, we need to break at newlines in orig
upper_mask = frame.orig~=lower(frame.orig);
for layerIdx = 1:numLayers
    tr.orig(layerIdx,:) = frame.orig;
    tr.orig(layerIdx, frame.nsnp_idxs_in_orig_frame>0) = tr.nsnpl(layerIdx,:);
    tr.orig(layerIdx, upper_mask) = upper(tr.orig(layerIdx,upper_mask));
end

lf = strfind(frame.orig, char(10));
starts = [1,lf(1:end-1)+1];
fins = lf;
tr.orig3d = [];
for chunk_idx = 1:length(starts)
    for layerIdx = 1:numLayers
        tr.orig3d = [tr.orig3d, tr.orig(layerIdx, starts(chunk_idx):fins(chunk_idx))];
    end
end
    