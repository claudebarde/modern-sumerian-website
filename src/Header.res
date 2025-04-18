@react.component
let make = () => {
    <header>
        <h1>
            <span>
                {"Modern Sumerian"->React.string}
            </span>
            <span className="cuneiform">
                {["eme", "ĝir15"]
                ->WebUtils.displayCuneiforms
                ->Js.Array2.joinWith("")
                ->React.string}
            </span>
        </h1>
    </header>
}