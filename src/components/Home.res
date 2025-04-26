@module external styles: {..} = "./Home.module.scss"

@react.component
let make = () => {
    <div className={styles["home"]}>
        <p>{"Coming soon"->React.string}</p>
        <p className="cuneiforms">{["ul", "la", " ", "im", "ĝen"]
            ->WebUtils.displayCuneiforms
            ->Array.map(((cuneiform, _)) => cuneiform)
            ->Js.Array2.joinWith("")
            ->React.string}
        </p>
    </div>
}