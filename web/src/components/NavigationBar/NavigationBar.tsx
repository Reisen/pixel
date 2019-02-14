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
        <Link to="/">
            PIXEL - {props.username}
        </Link>
    </div>
);

const mapState = (state: State) => ({
    username: state.user.username
});

export default connect(mapState)(NavigationBar);
