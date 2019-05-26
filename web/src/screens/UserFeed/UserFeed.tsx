// This Screen allows registration, the reason we do this instead of
// sharing the login screen/components is the goal to display information
// about the project during registration. This means the screens are more
// dissimilar than just a pair of forms.

import React            from 'react';
// import { User as user } from '../../api/types';
import { connect }      from 'react-redux';
import { State }        from '../../store';
import { History }      from 'history'
import { registerUser } from '../../store/users';

// Components
import NavigationBar    from '../../components/NavigationBar';
import styles           from './UserFeed.module.css';


interface Props {
    registerUser: (email: string, password: string) => Promise<Response>;
    username:     string;
    history:      History;
}

const headerLinks = [
    {name: 'Login', path: ''},
    {name: 'Register', path: ''},
];

const UserFeed = (props: Props) => {
    return (
        <div className="Page">
            <NavigationBar links={headerLinks} username={props.username} />
            <div className={styles.Root}>
                <img
                    alt="Avatar"
                    className={styles.Avatar}
                    src="https://i.chzbgr.com/full/8820501760/h4A394CEE/"
                />

                <div className={styles.Bio}>
                    <h1>Admin Dog</h1>
                </div>
            </div>

            <div className={styles.NavigationBar}>
                <span className={styles.Active}>
                    <i className="icofont-image"/>
                    Feed
                </span>

                <span>
                    <i className="icofont-heart-alt"/>
                    Favorites
                </span>
            </div>

            <div className={styles.Feed}>
                <div className={styles.FeedImage}></div>
                <div className={styles.FeedImage}></div>
                <div className={styles.FeedImage}></div>
                <div className={styles.FeedImage}></div>
                <div className={styles.FeedImage}></div>

                <div className={styles.FeedImage}></div>
                <div className={styles.FeedImage}></div>
                <div className={styles.FeedImage}></div>
                <div className={styles.FeedImage}></div>
                <div className={styles.FeedImage}></div>

                <div className={styles.FeedImage}></div>
                <div className={styles.FeedImage}></div>
                <div className={styles.FeedImage}></div>
                <div className={styles.FeedImage}></div>
                <div className={styles.FeedImage}></div>
            </div>
        </div>
    );
};

const mapState = (state: State) => ({
    username: state.user.username
});

const mapDispatch = {
    registerUser
};

export default connect(mapState, mapDispatch)(UserFeed);
