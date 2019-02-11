import React  from 'react';
import styles from './Image.module.css';

interface Props {
    empty?: boolean;
    resolution?: string;
}

const Image = (props: Props) => {
    const classes = [
        styles['Image'],
        props.empty ? styles['Image--empty'] : ""
    ].join(' ');

    return (
        <div className={classes}>
            <span className={styles.Resolution}>
                {props.resolution}
            </span>
        </div>
    );
}

export default Image;
