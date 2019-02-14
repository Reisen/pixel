import React       from 'react';
import styles      from './NavigationBar.module.css';
import { Link }    from 'react-router-dom';
import { connect } from 'react-redux';
import { State }   from '../../store/reducers';

interface Props {
    username: string;
}

const NavigationBar = (props: Props) => (
    <div className={styles.NavigationBar}>
        <Link className={styles.Logo} to="/">PIXEL</Link>

        <div className={styles.RightNavigation}>
            <Link to="/u/acb38921-9ab39ab1-112cb1212-90bfe32">{props.username}</Link>
            <Link to="/u/acb38921-9ab39ab1-112cb1212-90bfe32">Logout</Link>
        </div>
    </div>
);

export default connect(
    (state: State) => ({
        username: state.user.username
    })
)(NavigationBar);
