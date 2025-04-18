@module external styles: {..} = "./Home.module.scss"

@react.component
let make = () => {
    <div className={styles["home"]}>
        <p>{"Coming soon"->React.string}</p>
        <p className="cuneiform">{["ul", "la", " ", "im", "ĝen"]
            ->WebUtils.displayCuneiforms
            ->Js.Array2.joinWith("")
            ->React.string}</p>
    </div>
}