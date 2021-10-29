function [subsMat3d, invMat3d, nsnpIdxSubs3d, nsnpIdxInv3d] = build3dSubs(subT,subsMatStart)
% Build 3D substitution matrix, numAlphas by 26. 
%
% subsMatStart can be a scalar of number of cipher alphabets to start with
% a blank one, or can be an existing one to update

if numel(subsMatStart)==1
    numAlphas = subsMatStart;
    subsMatStart = repmat('_',numAlphas,26);
end
% Create inverse from scratch (NB: assymmetry with subsMat, where can start
% from partially filled one already)
invMat3d = repmat('_',size(subsMatStart,1),size(subsMatStart,2));

alphabet = 'abcdefghijklmnopqrstuvwxyz';

% fcn to convert letter to place in alphabet (a to 1 , b to 2, etc)
toLettIdx = @(c) double(lower(c))-96; 

subsMat3d = subsMatStart; % copy
nsnpIdxSubs3d = zeros(size(subsMat3d));
nsnpIdxInv3d = zeros(size(subsMat3d));

for rIdx = 1:size(subT,1)
    row = subT(rIdx,:);
    nsnp_idx = row.nsnp_idx;
    alph_num = row.alph_num;
    plain_lett = row.plain_lett;
    ciph_lett = row.ciph_lett;
    ciph_lett_idx = toLettIdx(ciph_lett); 
    existing_plains = squeeze(subsMat3d(alph_num, ciph_lett_idx, :));
    plain_lett_idx = toLettIdx(plain_lett);
    existing_ciphs = squeeze(invMat3d(alph_num, plain_lett_idx, :));
    % Add to subsMat3d if needed
    if any(plain_lett==existing_plains)
        % already accounted for, no need to do anything
    else
        % Add to subsMat3d
        if existing_plains(1)=='_'
            cur_num_plains = 0;
        else%if ~any(plain_lett==existing_plains)
            % Add new plain letter on a new layer
            cur_num_plains = sum(existing_plains~=char(0));
        end
        subsMat3d(alph_num, ciph_lett_idx, cur_num_plains+1) = plain_lett;
        nsnpIdxSubs3d(alph_num, ciph_lett_idx, cur_num_plains+1) = nsnp_idx; 
    end
    % Add to invMat3d if needed
    if any(ciph_lett==existing_ciphs)
        % no need to do anything
    else
        % Add to invMat3d
        if existing_ciphs(1)=='_'
            cur_num_ciphs = 0;
        else
            cur_num_ciphs = sum(existing_ciphs~=char(0));
        end
        invMat3d(alph_num, plain_lett_idx, cur_num_ciphs+1) = ciph_lett;
        nsnpIdxInv3d(alph_num, plain_lett_idx, cur_num_ciphs+1) = nsnp_idx; 
    end
end
