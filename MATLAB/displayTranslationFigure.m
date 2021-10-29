function displayTranslationFigure(frame,tr,figNum)

if nargin<3 || isempty(figNum)
    figNum=100;
end
% Create figure
fig = figure(figNum);

left = 400;
bot = 900;
wid = 900; % 450 per column works ok
ht = 1250;

e1 = uicontrol('Style','edit','Max',2);
e1.Position = [10,20,400,1200];
e1.FontName = 'Courier';
e1.HorizontalAlignment = 'left';
e1.String = frame.orig(frame.orig~=char(13)); % strip carriage returns

e2 = uicontrol('Style','edit','Max',2);
e2.Position = [420,20,450,1200];
e2.FontName = 'Courier';
e2.HorizontalAlignment = 'left';
e2.String = tr.oslash(tr.oslash~=char(13)); % strip carriage returns





