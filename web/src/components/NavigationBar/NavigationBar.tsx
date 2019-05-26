import React       from 'react';
import styles      from './NavigationBar.module.css';
import { Link }    from 'react-router-dom';


interface Props {
    onLogout?: () => void;
    username?: string;
    links:     { name: string, path: string }[];
}


const NavigationBar = (props: Props) => {
    const LoggedInFragment = () => (
        <React.Fragment>
            <Link to="/u/acb38921-9ab39ab1-112cb1212-90bfe32">
                {props.username}
            </Link>

            <a href="/my/logout" onClick={props.onLogout}>
                Logout
            </a>
        </React.Fragment>
    );

    const LoggedOutFragment = () => (
        <React.Fragment>
            <Link to="/my/login">
                Login
            </Link>
        </React.Fragment>
    );

    return (
        <React.Fragment>
            <div className={styles.NavigationBar}>
                <div className={styles.InnerRoot}>
                    <Link className={styles.Logo} to="/">PIXEL</Link>

                    <div className={styles.RightNavigation}>
                        <Link to="/my/upload" className={styles.UploadLink}>
                            Upload Images
                        </Link>

                        { props.username  && <LoggedInFragment /> }
                        { !props.username && <LoggedOutFragment /> }
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
};


export default NavigationBar;
