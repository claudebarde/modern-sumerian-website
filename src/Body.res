@module external styles: {..} = "./Body.module.scss"

@react.component
let make = () => {
    let url = RescriptReactRouter.useUrl()
    
    <div className={styles["body"]}>
        {
            switch url.path {
                | list{"conjugator"} => <Conjugator />
                | list{"cuneiforms"} => <Cuneiforms />
                | list{"links"} => <Links />
                | list{} | list{"home"} => <Home />
                | _ => <PageNotFound/>
            }
        }
    </div>
}