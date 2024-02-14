using VectSharp;
using VectSharp.SVG;

Document document = new Document();
Page page = new Page(1000, 1000);
document.Pages.Add(page);

page.Graphics.FillRectangle(100, 100, 800, 50, Colour.FromRgb(128, 128, 128), tag: "linkToGitHub");
page.Graphics.FillRectangle(100, 300, 800, 50, Colour.FromRgb(255, 0, 0), tag: "linkToBlueRectangle");
page.Graphics.FillRectangle(100, 850, 800, 50, Colour.FromRgb(0, 0, 255), tag: "blueRectangle");

page.SaveAsSVG(@"doc.svg");