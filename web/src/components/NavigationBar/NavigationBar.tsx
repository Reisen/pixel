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
                <Link className={styles.Logo} to="/">P</Link>

                <Link to="/" className={styles.Button}>
					<span>
						<i className="icofont-home"></i>
					</span>
				</Link>

                <Link to="/my/upload" className={styles.Button}>
					<span>
						<i className="icofont-cloud-upload"></i>
					</span>
				</Link>
            </div>
        </React.Fragment>
    );
};


export default NavigationBar;
