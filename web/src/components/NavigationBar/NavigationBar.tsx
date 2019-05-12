import React       from 'react';
import styles      from './NavigationBar.module.css';
import { Link }    from 'react-router-dom';


interface Props {
    username: string;
    links:    {name: string, path: string}[];
}


const NavigationBar = (props: Props) => (
    <React.Fragment>
        <div className={styles.NavigationBar}>
            <div className={styles.InnerRoot}>
                <Link className={styles.Logo} to="/">PIXEL</Link>

                <div className={styles.RightNavigation}>
                    <Link to="/my/upload" className={styles.UploadLink}>Upload Images</Link>
                    <Link to="/u/acb38921-9ab39ab1-112cb1212-90bfe32">{props.username}</Link>
                    <Link to="/u/acb38921-9ab39ab1-112cb1212-90bfe32">Logout</Link>
                </div>
            </div>
        </div>

        <div className={styles.SubNavigationBar}>
            <div className={styles.InnerRoot}>
                <div className={styles.LeftNavigation}>
                    {
                        props.links.map(link =>
                            <Link key={link.name} to={link.path}>{link.name}</Link>
                        )
                    }
                </div>
            </div>
        </div>
    </React.Fragment>
);


export default NavigationBar;
