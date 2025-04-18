@module external styles: {..} = "./BodyNav.module.scss"

@react.component
    let make = () => {
        <nav className={styles["navColumn"]}>
            <ul>
                <li>
                    <button
                        className={styles["navButton"]}
                        onClick={_ => {
                            RescriptReactRouter.push("/")
                        }}>
                        {"Home"->React.string}
                    </button>
                </li>
                <li>
                    <button
                        className={styles["navButton"]}
                        onClick={_ => {
                            RescriptReactRouter.push("conjugator")
                        }}>
                        {"Conjugator"->React.string}
                    </button>
                </li>
                <li>
                    <button
                        className={styles["navButton"]}
                        onClick={_ => {
                            RescriptReactRouter.push("links")
                        }}>
                        {"Links"->React.string}
                    </button>
                </li>
            </ul>
        </nav>
}