import React  from 'react';
import styles from './Image.module.css';

interface Props {
    resolution: string;
}

const Image = (props: Props) => (
    <div className={styles.Image}>
        <span className={styles.Resolution}>
            {props.resolution}
        </span>
    </div>
);

export default Image;
