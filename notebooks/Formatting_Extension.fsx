#r "nuget: Newtonsoft.Json"
#r "nuget: FsLab"
#r "nuget: FSharp.Plotly"

open Deedle
open Newtonsoft.Json
open System
open System.Web
open System.Text
open FSharp.Plotly
open FSharp.Plotly.GenericChart
open FSharp.Plotly.StyleParam

let chart =
    let newScript = new StringBuilder()
    newScript.AppendLine("""<div id="[ID]" style="width: [WIDTH]px; height: [HEIGHT]px;"><!-- Plotly chart will be drawn inside this DIV --></div>""") |> ignore
    newScript.AppendLine("<script type=\"text/javascript\">") |> ignore
    newScript.AppendLine(@"
var renderPlotly = function() {
    var xplotRequire = requirejs.config({context:'xplot-3.0.1',paths:{plotly:'https://cdn.plot.ly/plotly-1.49.2.min'}});
    xplotRequire(['plotly'], function(Plotly) {")  |> ignore
    newScript.AppendLine(@"
var data = [DATA];
var layout = [LAYOUT];
var config = [CONFIG];
Plotly.newPlot('[ID]', data, layout, config);")  |> ignore
    newScript.AppendLine("""});
};
if ((typeof(requirejs) !==  typeof(Function)) || (typeof(requirejs.config) !== typeof(Function))) { 
    var script = document.createElement("script"); 
    script.setAttribute("src", "https://cdnjs.cloudflare.com/ajax/libs/require.js/2.3.6/require.min.js"); 
    script.onload = function(){
        renderPlotly();
    };
    document.getElementsByTagName("head")[0].appendChild(script); 
}
else {
    renderPlotly();
}""") |> ignore
    newScript.AppendLine("</script>") |> ignore
    newScript.ToString()
    //|> HtmlString

/// Converts a GenericChart to it HTML representation. The div layer has a default size of 600 if not specified otherwise.
let toChartHTML gChart =
    let guid = Guid.NewGuid().ToString()
    let tracesJson =
        getTraces gChart
        |> JsonConvert.SerializeObject 
    let layoutJson = 
        getLayout gChart
        |> JsonConvert.SerializeObject 
    let configJson =
        getConfig gChart
        |> JsonConvert.SerializeObject 

    let dims = tryGetLayoutSize gChart
    let width,height =
        match dims with
        |Option.Some (w,h) -> w,h
        |Option.None  -> 600., 600.

    let html =
        chart(*.ToHtmlString()*)
            //.Replace("style=\"width: [WIDTH]px; height: [HEIGHT]px;\"","style=\"width: 600px; height: 600px;\"")
            .Replace("[WIDTH]", string width)
            .Replace("[HEIGHT]", string height)
            .Replace("[ID]", guid)
            .Replace("[DATA]", tracesJson)
            .Replace("[LAYOUT]", layoutJson)
            .Replace("[CONFIG]", configJson)
    html    


let formatAsTable maxRows maxCols (f:Frame<_,_>) =
    let maxColsIdx = System.Math.Min(f.ColumnCount,maxCols)     
    let maxRowIdx = System.Math.Min(f.RowCount,maxRows) 
    let header = 
        f.ColumnKeys 
        |> Seq.take maxColsIdx
        |> Seq.append ["RowKey"]
    let f' =
        f
        |> Frame.sliceCols header
        |> fun f' -> 
            if f'.ColumnCount < f.ColumnCount then 
                f'
                |> Frame.addCol "..." (f'.RowKeys |> Seq.map (fun ck -> ck,"...") |> Series.ofObservations)
            else 
                f'

    let columnWidth = 
        let headerLength = 
            header 
            |> Seq.map (fun (x:string) -> (x.Length*10) + 10)
        let colLenght    =
            f'
            |> Frame.getCols 
            |> Series.values 
            |> Seq.map (fun s -> 
                s 
                |> Series.values 
                |> Seq.map (string >> String.length >> float) 
                |> Seq.average 
                |> int
                )
        Seq.map2 (fun (x:int) (y:int) -> System.Math.Max(x,y)) headerLength colLenght
    let rows = 
        f'    
        |> Frame.mapRows (fun k s -> s.As<string>() |> Series.values |> Seq.append [k.ToString()])
        |> Series.values
        |> Seq.take maxRowIdx
    Chart.Table(
        header,
        //rows
        rows,
        //sets global header alignment
        AlignHeader = [HorizontalAlign.Center],
        //sets alignment for each column separately 
        //(The last alignment is applied to all potential following columns)
        //AlignCells  = [HorizontalAlign.Left;HorizontalAlign.Center;HorizontalAlign.Right],
        AlignCells  = [HorizontalAlign.Center],
        //sets global header color
        ColorHeader = "#45546a",    
        //sets specific color to each header column
        //ColorHeader=["#45546a";"#deebf7";"#45546a";"#deebf7"],    
        //sets global cell color
        //ColorRows = "#deebf7",
        //sets cell column colors
        ColorCells  = (header |> Seq.mapi (fun i x -> if i%2 = 0 then  "#deebf7" else "lightgrey")),
        //sets cell row colors
        //ColorCells=[["#deebf7";"lightgrey"]],
        //sets font of header
        FontHeader  = Font.init(FontFamily.Courier_New, Size=12, Color="white"),      
        //sets the height of the header
        HeightHeader= 30.,
        //sets lines of header
        LineHeader  = Line.init(2.,"black"),                    
        ColumnWidth = columnWidth      
        //defines order of columns
        //ColumnOrder = [1;2;3;4]                                  
        )
    |> Chart.withSize((columnWidth |> Seq.sum |> float |> (*) 2.),500.)
    |> toChartHTML
    
// transmission and recovery rate per time interval (r_0 =  transmissionRate/recoveryRate)
let plotTimeCourse maxT s' i' r' susceptibleStart infectedStart recoveredStart transmissionRate recoveryRate = 
    let (time,susceptible,infected,recovered)  =
        let rec loop t s i r accT accS accI accR =
            let stepwidth = 0.01
            if t > maxT + 1. then 
                accT |> List.rev, 
                accS |> List.rev, 
                accI |> List.rev, 
                accR |> List.rev 
            else 
                let nS = s + (s' s i transmissionRate) * stepwidth
                let nI = i + (i' s i transmissionRate recoveryRate) * stepwidth
                let nR = r + (r' i recoveryRate) * stepwidth
                loop (t + stepwidth) nS nI nR (t::accT) (nS::accS) (nI::accI) (nR::accR) 

        loop 0. susceptibleStart infectedStart recoveredStart [] [] [] []
    [    
        Chart.Line(time,susceptible,Color="#ff7f0e") |> Chart.withTraceName "susceptible"
        Chart.Line(time,infected,Color="#d62728")    |> Chart.withTraceName (sprintf "infected tR=%.3f rR=%.3f" transmissionRate recoveryRate)
        Chart.Line(time,recovered,Color="#2ca02c")   |> Chart.withTraceName "recovered"
    ]
    |> Chart.Combine
    |> Chart.withX_AxisStyle "time interval"
    |> Chart.withX_AxisStyle "percentage of population"
    |> Chart.withSize((columnWidth |> Seq.sum |> float |> (*) 2.),500.)
    
 let plotTimeCourseDash maxT s' i' r' susceptibleStart infectedStart recoveredStart transmissionRate recoveryRate = 
    plotTimeCourse transmissionRate recoveryRate
    |> Chart.withLineStyle(Dash=StyleParam.Dash)
    
//Formatter<Frame<_,_>>.Register((fun f writer -> 
//    let table1 = Chart.Table(header, rows)
//    writer.Write(table1 |> toChartHTML |> HtmlString  )
//                               ), mimeType = "text/html")

//Formatter<GenericChart.GenericChart>.Register(
//    fun ch writer ->
//        writer.Write(toJupyterCompatibleHTML ch)
//    , mimeType = "text/html");


//Formatter<Frame<int,string>>.Register(
//    fun frame writer ->
//        writer.Write(frame.Print())
//    , mimeType = "text/html");

//type WOLOLO = 
//    | Red * of int
//    | Blue of int
//Formatter<WOLOLO>.Register( (fun ch writer ->                                                 
//                        writer.Write("hahahaaaa")
//                   ), mimeType = "text/html") 
//let x = WOLOLO.Red 5 
//x
