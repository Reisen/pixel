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
                <div className={styles.Stages}>
                    <div className={styles.Stage}>
                        Upload Images
                    </div>

                    <div className={styles.Stage}>
                        Tags
                    </div>

                    <div className={styles.Stage}>
                        Galleries
                    </div>

                    <div className={styles.Stage}>
                    </div>
                </div>

                <div className={styles.UploadBox}>
                    Upload Boxes
                </div>
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
