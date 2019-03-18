import React       from 'react';
import styles      from './ImagePanel.module.css';
import { image }   from '../../../../types/image';
import { repeat }  from 'ramda';

const galleries = repeat(
    {
        name: "Art",
        image: "https://i.imgur.com/6qvrOr1.jpg"
    }, 15
);

interface Props {
    image: image
}

export default (props: Props) =>
    <div className={styles.Root}>
        <h1>Currently In</h1>
        {
            galleries.slice(4, 7).map(gallery =>
                <div style={{backgroundImage: `url(${gallery.image})` }} className={styles.Gallery}>
                    <span className={styles.Name}>
                        <i className={`icofont-check-circled`}/>
                        {gallery.name}
                    </span>
                </div>
            )
        }

        <h1>Galleries Available</h1>
        {
            galleries.map(gallery =>
                <div style={{backgroundImage: `url(${gallery.image})` }} className={styles.Gallery}>
                    <span className={styles.Name}>
                        {gallery.name}
                    </span>
                </div>
            )
        }
    </div>;
