@module external styles: {..} = "./Body.module.scss"

@react.component
let make = () => {
    let url = RescriptReactRouter.useUrl()
    
    <div className={styles["body"]}>
        <BodyNav />
        {
            switch url.path {
                | list{"conjugator"} => <Conjugator />
                | list{"links"} => <Links />
                | list{} | list{"home"} => <Home />
                | _ => <PageNotFound/>
            }
        }
    </div>
}