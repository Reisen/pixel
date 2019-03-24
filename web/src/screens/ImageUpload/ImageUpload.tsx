import React           from 'react';
import { State }       from '../../store';
import { connect }     from 'react-redux';
import { fetchImages } from '../../store/images';

import NavigationBar   from '../../components/NavigationBar';
import styles          from './ImageUpload.module.css';


interface Props {
    username: string;
}

const Image = (props: Props) => {
    return (
        <div className="Page">
            <NavigationBar username={props.username} />

            <div className={styles.Root}>
            </div>
        </div>
    );
}

const mapState = (state: State) => ({
    username: state.user.username
});

const mapDispatch = {
    fetchImages
};


export default connect(mapState, mapDispatch)(Image);
