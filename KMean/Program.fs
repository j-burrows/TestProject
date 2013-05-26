// Learn more about F# at http://fsharp.net

//Here is a breif introduction 
type Input<'T> = {Data: 'T; Features:float[]}
type Centroid = float[]

module Array =
    let classifyBy f (xs : _[]) =
        xs |> Seq.groupBy f |> Seq.map (fun (k,v) -> (k,Seq.toArray v)) |> Seq.toArray

module Seq =
    let iterate f x = x |> Seq.unfold (fun x->Some(x,f x))

let distance (xs:Input <_>)(ys: Centroid) =
    (xs.Features,ys)
        ||> Array.map2 (fun x y -> (x - y) * (x - y))
        |> Array.sum

///Find the average of set inputs. First compute xs1+xs2...+xsn
///Then divide each element of the sum by the number of inputs
let computeCentroidOfGroup(_,group:Input<_>[])=
    let eO = group.[0].Features
    [|for i in 0..eO.Length - 1 -> group |> Array.averageBy (fun e -> e.Features.[i])|]

///Group all the inputs by the nearest centroid
let classifyIntoGroups inputs centroids =
    inputs |> Array.classifyBy (fun v -> centroids |> Array.minBy (distance v))

///Repeatedly classify inputs, starting with the initial centroids
let rec computeCentroids inputs centroids = seq {
    let classification = classifyIntoGroups inputs centroids
    yield classification
    let newCentroids = Array.map computeCentroidOfGroup classification
    yield! computeCentroids inputs newCentroids}


///Extract the features and repeatedly classify the inputs, starting with the
///initial centroids
let kmeans inputs featureExtractor initialCentroids =
    let inputs =
        inputs
        |> Seq.map (fun i-> {Data = i; Features = featureExtractor i})
        |> Seq.toArray
    let initialCentroids = initialCentroids |> Seq.toArray
    computeCentroids inputs initialCentroids