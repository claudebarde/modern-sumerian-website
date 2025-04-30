@react.component
let make = (~codePoint: string, ~pronunciation: string) => {
    let element = <span className="cuneiforms">
        {codePoint->React.string}
    </span>

    React.cloneElement(element, {"data-tooltip": pronunciation})
}