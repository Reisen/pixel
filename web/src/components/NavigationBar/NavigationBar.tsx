import React       from 'react';
import IconButton  from '../IconButton';
import styles      from './NavigationBar.module.css';
import { Link }    from 'react-router-dom';


interface Props {
    username: string;
}


const NavigationBar = (props: Props) => (
    <div className={styles.Root}>
        <div className={styles.InnerRoot}>
            <Link className={styles.Logo} to="/">PIXEL</Link>

            <div className={styles.RightNavigation}>
                <Link to="/u/acb38921-9ab39ab1-112cb1212-90bfe32">{props.username}</Link>
                <Link to="/u/acb38921-9ab39ab1-112cb1212-90bfe32">Logout</Link>
            </div>
        </div>
    </div>
);


export default NavigationBar;
