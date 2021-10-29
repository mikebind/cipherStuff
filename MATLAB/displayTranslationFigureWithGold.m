function displayTranslationFigureWithGold(frame,tr,goldo,figNum)

if nargin<4 || isempty(figNum)
    figNum=101;
end
% Create figure
fig = figure(figNum);

left = 400;
bot = 900;
wid = 450*3; % 450 per column works ok
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

e3 = uicontrol('Style', 'edit', 'Max',2);
e3.Position = [420+450+10, 20, 450, 1200];
e3.FontName = 'Courier';
e3.HorizontalAlignment = 'left';
e3.String = goldo(goldo~=char(13));


