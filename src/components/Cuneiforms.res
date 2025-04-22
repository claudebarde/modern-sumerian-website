@module external styles: {..} = "./Cuneiforms.module.scss"

@react.component
let make = () => {
    <div className={styles["cuneiforms"]}>
        <input
            type_="text"
            placeholder="Enter Sumerian words here"
        />
        <div>
            {"Options"->React.string}
        </div>
        <div>
            {"Output"->React.string}
        </div>
    </div>
}